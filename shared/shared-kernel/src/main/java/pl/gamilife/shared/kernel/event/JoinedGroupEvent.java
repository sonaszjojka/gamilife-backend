package pl.gamilife.shared.kernel.event;

import java.util.Collection;
import java.util.UUID;

public record JoinedGroupEvent(
        UUID userId,
        String username,
        boolean isFirstTimeJoin,
        UUID groupId,
        String groupName,
        Collection<UUID> activeMembersUserIds
) {
}
