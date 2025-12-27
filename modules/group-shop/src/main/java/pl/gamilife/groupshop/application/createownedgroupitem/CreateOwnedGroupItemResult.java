package pl.gamilife.groupshop.application.createownedgroupitem;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record CreateOwnedGroupItemResult(
        UUID ownedGroupItemId,
        UUID groupItemId,
        UUID groupMemberId,
        Instant acquiredAt
) {
}
