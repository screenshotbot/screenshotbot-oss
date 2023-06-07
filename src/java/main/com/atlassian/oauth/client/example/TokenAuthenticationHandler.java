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
public class TokenAuthenticationHandler implements AuthenticationHandler {
    private String token;
    public TokenAuthenticationHandler(String token) {
        this.token = token;
    }

    @Override
    public void configure(final Request.Builder builder) {
        builder.setHeader("Authorization",
                          String.format("Bearer %s", token));
    }
}
