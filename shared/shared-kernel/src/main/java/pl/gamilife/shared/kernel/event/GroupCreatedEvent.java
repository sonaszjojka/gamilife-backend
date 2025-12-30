package pl.gamilife.shared.kernel.event;

import java.util.UUID;

public record GroupCreatedEvent(
        UUID groupId,
        String groupName
) {
}
