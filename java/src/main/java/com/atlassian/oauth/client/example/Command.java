package com.atlassian.oauth.client.example;

import java.util.Arrays;
import java.util.stream.Collectors;

public enum Command {
    REQUEST_TOKEN("requestToken"),
    ACCESS_TOKEN("accessToken"),
    REQUEST("request");

    private final String name;

    Command(final String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }

    public static String names() {
        return Arrays.asList(values()).stream()
                .map(Command::getName)
                .collect(Collectors.toList())
                .toString();
    }

    public static Command fromString(String name) {
        if (name != null) {
            for (Command b : Command.values()) {
                if (name.equalsIgnoreCase(b.name)) {
                    return b;
                }
            }
        }
        return null;
    }
}