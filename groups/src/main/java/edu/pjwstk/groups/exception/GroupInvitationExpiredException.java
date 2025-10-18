package edu.pjwstk.groups.exception;

public class GroupInvitationExpiredException extends RuntimeException {
    public GroupInvitationExpiredException(String message) {
        super(message);
    }
}
