package pl.gamilife.groupshop.application.editownedgroupitem;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record EditOwnedGroupItemResponse(
        UUID ownedGroupItemId,
        UUID groupItemId,
        UUID groupMemberId,
        Boolean isUsedUp,
        Instant useDate
) {


}
