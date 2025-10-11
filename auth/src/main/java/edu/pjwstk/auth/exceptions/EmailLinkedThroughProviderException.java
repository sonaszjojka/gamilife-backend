package edu.pjwstk.auth.exceptions;

public class EmailLinkedThroughProviderException extends RuntimeException {
    private final String linkToken;
    public EmailLinkedThroughProviderException(String message, String linkToken) {
        super(message);
        this.linkToken = linkToken;
    }

    public String getLinkToken() {
        return linkToken;
    }
}
