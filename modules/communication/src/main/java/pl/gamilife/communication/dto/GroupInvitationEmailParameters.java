package pl.gamilife.communication.dto;

import pl.gamilife.communication.enums.EmailType;

import java.util.Map;
import java.util.UUID;

public record GroupInvitationEmailParameters(
        UUID groupId,
        String groupName,
        UUID groupInvitationId,
        String token
) implements EmailParameters {
    @Override
    public EmailType getEmailType() {
        return EmailType.GROUP_INVITATION;
    }

    @Override
    public Map<String, String> getParametersMap() {
        return Map.of(
                "groupId", groupId.toString(),
                "groupName", groupName,
                "groupInvitationId", groupInvitationId.toString(),
                "token", token
        );
    }
}
