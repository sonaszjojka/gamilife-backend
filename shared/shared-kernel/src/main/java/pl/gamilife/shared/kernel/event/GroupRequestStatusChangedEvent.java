package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record GroupRequestStatusChangedEvent(
        UUID requesterUserId,
        boolean accepted,
        String groupName,
        UUID groupId
) {
}
