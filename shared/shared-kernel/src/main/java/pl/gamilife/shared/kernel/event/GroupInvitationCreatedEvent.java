package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record GroupInvitationCreatedEvent(
        UUID userId,
        UUID groupId,
        String groupName,
        UUID groupInvitationId,
        String token
) {
}
