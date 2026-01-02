package pl.gamilife.shared.kernel.event;

import java.util.Collection;
import java.util.UUID;

public record GroupMemberLeftEvent(
        UUID userId,
        UUID groupId,
        String groupName,
        Collection<UUID> activeMembersUserIds
) {
}
