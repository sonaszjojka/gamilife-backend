package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record GroupRequestCreatedEvent(
        UUID adminId,
        UUID requesterUserId,
        String groupName,
        UUID groupId
) {
}
