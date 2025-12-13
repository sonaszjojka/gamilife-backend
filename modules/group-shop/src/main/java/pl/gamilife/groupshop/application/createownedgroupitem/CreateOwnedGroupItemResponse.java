package pl.gamilife.groupshop.application.createownedgroupitem;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record CreateOwnedGroupItemResponse(
        UUID ownedGroupItemId,
        UUID groupItemId,
        UUID groupMemberId,
        Boolean isUsedUp,
        Instant useDate
) {
}
