package edu.pjwstk.communication.dto;

import edu.pjwstk.communication.enums.EmailType;

public record GroupInvitationEmailParameters(String joinCode, String invitationLink) implements EmailParameters {
    @Override
    public EmailType getEmailType() {
        return EmailType.GROUP_INVITATION;
    }
}
