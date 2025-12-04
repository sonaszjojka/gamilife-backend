package pl.gamilife.groupshop.usecase.createownedgroupitem;

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
