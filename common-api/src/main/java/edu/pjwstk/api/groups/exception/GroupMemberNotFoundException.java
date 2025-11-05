package edu.pjwstk.api.groups.exception;

public class GroupMemberNotFoundException extends RuntimeException {
    public GroupMemberNotFoundException(String message) {
        super(message);
    }
}
