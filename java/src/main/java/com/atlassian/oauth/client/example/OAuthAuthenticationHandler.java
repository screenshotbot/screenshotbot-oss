package com.atlassian.oauth.client.example;

import com.google.api.client.auth.oauth.OAuthAuthorizeTemporaryTokenUrl;
import com.google.api.client.auth.oauth.OAuthCredentialsResponse;
import com.google.api.client.auth.oauth.OAuthParameters;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.security.spec.InvalidKeySpecException;
import com.atlassian.jira.rest.client.api.AuthenticationHandler;
    import com.atlassian.httpclient.api.Request;

import com.google.api.client.http.GenericUrl;
import java.security.*;

import static com.atlassian.oauth.client.example.PropertiesClient.JIRA_HOME;

/**
 * Not part of original SDK. Built by arnold.
 */
public class OAuthAuthenticationHandler implements AuthenticationHandler {
    private OAuthParameters parameters;
    public OAuthAuthenticationHandler(OAuthParameters parameters) {
        this.parameters = parameters;
    }
    @Override
    public void configure(final Request.Builder builder) {
        parameters.computeNonce();
        parameters.computeTimestamp();
        Request r = builder.build();
        GenericUrl genericUrl = new GenericUrl(r.getUri());
        System.out.println("From java: " + genericUrl + " " + r.getMethod().toString());
        try {
            parameters.computeSignature(r.getMethod().toString(), genericUrl);
        } catch (GeneralSecurityException e) {
            throw new RuntimeException(e);
        }
        builder.setHeader("Authorization", parameters.getAuthorizationHeader());
    }
}
