package com.atlassian.oauth.client.example;

import java.util.Arrays;
import java.util.List;

public class ClientMain {

    public static void main(String[] args) throws Exception {
        if (args.length == 0) {
            throw new IllegalArgumentException("No command specified. Use one of " + Command.names());
        }

        PropertiesClient propertiesClient = new PropertiesClient();
        JiraOAuthClient jiraOAuthClient = new JiraOAuthClient(propertiesClient);

        List<String> argumentsWithoutFirst = Arrays.asList(args).subList(1, args.length);

        new OAuthClient(propertiesClient, jiraOAuthClient).execute(Command.fromString(args[0]), argumentsWithoutFirst);
    }
}
