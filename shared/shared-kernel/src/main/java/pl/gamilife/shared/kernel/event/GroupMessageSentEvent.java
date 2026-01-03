package pl.gamilife.shared.kernel.event;

import java.util.Collection;
import java.util.UUID;

public record GroupMessageSentEvent(
        UUID userId,
        UUID groupId,
        String groupName,
        String message,
        boolean important,
        Collection<UUID> activeMembersUserIds
) {
}
