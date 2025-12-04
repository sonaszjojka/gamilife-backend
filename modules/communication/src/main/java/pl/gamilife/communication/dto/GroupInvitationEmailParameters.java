package pl.gamilife.communication.dto;

import pl.gamilife.communication.enums.EmailType;

public record GroupInvitationEmailParameters(String joinCode, String invitationLink) implements EmailParameters {
    @Override
    public EmailType getEmailType() {
        return EmailType.GROUP_INVITATION;
    }
}
