package edu.pjwstk.core.exception;

public enum CommonErrorCode implements ErrorCode {
    ;

    @Override
    public String getKey() {
        return this.name();
    }

    @Override
    public String getModule() {
        return "CORE";
    }
}
