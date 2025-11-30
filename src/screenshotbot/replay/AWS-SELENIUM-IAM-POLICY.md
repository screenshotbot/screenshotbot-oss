# IAM Policy for AWS Selenium Provider

## Overview

This document describes the IAM policy required for the `aws-selenium-provider` to securely manage EC2 instances for Selenium testing. The policy uses **tag-based access control** to restrict permissions to only instances created by the selenium provider.

## Security Model

All EC2 instances created by the `aws-selenium-provider` are tagged with:
- `ManagedBy=aws-selenium-provider`
- `Temporary=true`

The IAM policy restricts `ec2:TerminateInstances` permission to ONLY instances with these tags, preventing accidental termination of permanent infrastructure.

## IAM Policy

```json
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "AllowRunInstancesWithTags",
      "Effect": "Allow",
      "Action": "ec2:RunInstances",
      "Resource": [
        "arn:aws:ec2:*:*:instance/*",
        "arn:aws:ec2:*:*:volume/*"
      ],
      "Condition": {
        "StringEquals": {
          "aws:RequestTag/ManagedBy": "aws-selenium-provider",
          "aws:RequestTag/Temporary": "true"
        }
      }
    },
    {
      "Sid": "AllowRunInstancesOnOtherResources",
      "Effect": "Allow",
      "Action": "ec2:RunInstances",
      "Resource": [
        "arn:aws:ec2:*:*:subnet/*",
        "arn:aws:ec2:*:*:network-interface/*",
        "arn:aws:ec2:*:*:security-group/*",
        "arn:aws:ec2:*:*:key-pair/*",
        "arn:aws:ec2:*::image/*"
      ]
    },
    {
      "Sid": "AllowTerminateOnlyManagedInstances",
      "Effect": "Allow",
      "Action": "ec2:TerminateInstances",
      "Resource": "arn:aws:ec2:*:*:instance/*",
      "Condition": {
        "StringEquals": {
          "ec2:ResourceTag/ManagedBy": "aws-selenium-provider",
          "ec2:ResourceTag/Temporary": "true"
        }
      }
    },
    {
      "Sid": "AllowDescribeInstances",
      "Effect": "Allow",
      "Action": [
        "ec2:DescribeInstances",
        "ec2:DescribeInstanceStatus",
        "ec2:DescribeTags"
      ],
      "Resource": "*"
    },
    {
      "Sid": "AllowCreateTagsOnLaunch",
      "Effect": "Allow",
      "Action": "ec2:CreateTags",
      "Resource": "arn:aws:ec2:*:*:instance/*",
      "Condition": {
        "StringEquals": {
          "ec2:CreateAction": "RunInstances"
        }
      }
    },
    {
      "Sid": "AllowPassRoleForInstanceProfile",
      "Effect": "Allow",
      "Action": "iam:PassRole",
      "Resource": "arn:aws:iam::ACCOUNT-ID:role/YOUR-SELENIUM-INSTANCE-ROLE-NAME"
    }
  ]
}
```

## Configuration Steps

### 1. Replace Placeholders

In the policy above, replace:
- `ACCOUNT-ID` - Your AWS account ID
- `YOUR-SELENIUM-INSTANCE-ROLE-NAME` - The name of the IAM role that will be attached to the created EC2 instances

### 2. Create IAM Role for the Application

If running on EC2 (recommended):

```bash
# Create trust policy
cat > trust-policy.json <<EOF
{
  "Version": "2012-10-17",
  "Statement": [{
    "Effect": "Allow",
    "Principal": {
      "Service": "ec2.amazonaws.com"
    },
    "Action": "sts:AssumeRole"
  }]
}
EOF

# Create IAM role
aws iam create-role \
  --role-name SeleniumProviderRole \
  --assume-role-policy-document file://trust-policy.json

# Create and attach the policy
aws iam put-role-policy \
  --role-name SeleniumProviderRole \
  --policy-name SeleniumProviderPolicy \
  --policy-document file://selenium-provider-policy.json

# Create instance profile
aws iam create-instance-profile \
  --instance-profile-name SeleniumProviderInstanceProfile

# Add role to instance profile
aws iam add-role-to-instance-profile \
  --instance-profile-name SeleniumProviderInstanceProfile \
  --role-name SeleniumProviderRole

# Attach to your EC2 instance
aws ec2 associate-iam-instance-profile \
  --instance-id i-your-instance-id \
  --iam-instance-profile Name=SeleniumProviderInstanceProfile
```

### 3. Verify the Policy

Test that the policy works correctly:

```bash
# Should succeed - terminate a managed instance
aws ec2 terminate-instances --instance-ids i-managed-instance-id

# Should fail with AccessDenied - terminate a permanent instance
aws ec2 terminate-instances --instance-ids i-permanent-instance-id
```

## Policy Breakdown

### RunInstances Restrictions

The policy enforces that all instances created MUST have the required tags. This prevents privilege escalation where someone could create an instance without tags and then remove the tag-based restrictions.

```json
"Condition": {
  "StringEquals": {
    "aws:RequestTag/ManagedBy": "aws-selenium-provider",
    "aws:RequestTag/Temporary": "true"
  }
}
```

### TerminateInstances Restrictions

The critical security control - only instances with BOTH tags can be terminated:

```json
"Condition": {
  "StringEquals": {
    "ec2:ResourceTag/ManagedBy": "aws-selenium-provider",
    "ec2:ResourceTag/Temporary": "true"
  }
}
```

### DescribeInstances

Read-only permissions have no resource restrictions as they cannot cause harm:

```json
"Action": [
  "ec2:DescribeInstances",
  "ec2:DescribeInstanceStatus",
  "ec2:DescribeTags"
],
"Resource": "*"
```

## Additional Hardening (Optional)

For even stricter security, you can add additional conditions:

### Restrict to Specific Subnet

```json
{
  "Condition": {
    "StringEquals": {
      "ec2:Subnet": "arn:aws:ec2:us-east-1:123456789012:subnet/subnet-xxxxx"
    }
  }
}
```

### Restrict to Specific VPC

```json
{
  "Condition": {
    "StringEquals": {
      "ec2:Vpc": "arn:aws:ec2:us-east-1:123456789012:vpc/vpc-xxxxx"
    }
  }
}
```

### Restrict Instance Types

```json
{
  "Condition": {
    "StringEquals": {
      "ec2:InstanceType": ["m7a.medium", "m7a.large"]
    }
  }
}
```

## Troubleshooting

### Error: "You are not authorized to perform this operation"

This usually means:
1. The IAM role is not attached to your EC2 instance
2. The instance you're trying to terminate doesn't have the required tags
3. The IAM policy has a typo in the ARN or condition

### Instances Created Without Tags

If instances are being created without tags:
1. Check that the `--tag-specifications` parameter is correct in the code
2. Verify the IAM policy is enforcing the `aws:RequestTag` condition

### Cannot Create Instances

If you get permission denied when creating instances:
1. Ensure all resources (subnet, security groups, AMI, key pair) exist
2. Check that the IAM role specified in `iam-profile` exists and the policy allows `iam:PassRole`
3. Verify the `AllowRunInstancesOnOtherResources` statement includes all necessary resource types

## References

- [AWS IAM Policy Elements: Condition](https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_policies_elements_condition.html)
- [Controlling Access to EC2 Resources Using Tags](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/control-access-with-tags.html)
- [IAM Roles for Amazon EC2](https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/iam-roles-for-amazon-ec2.html)
