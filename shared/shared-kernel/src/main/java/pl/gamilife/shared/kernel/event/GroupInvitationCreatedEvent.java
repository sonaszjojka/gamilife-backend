package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record GroupInvitationCreatedEvent(
        UUID userId,
        String invitationLink,
        String groupName,
        String joinCode
) {
}
