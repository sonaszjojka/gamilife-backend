package edu.pjwstk.common.emailSenderApi;

public enum MailContentType {
    TEXT("text/plain"),
    HTML("text/html");

    private final String contentType;

    MailContentType(String contentType) {
        this.contentType = contentType;
    }

    public String getContentType() {
        return contentType;
    }
}
