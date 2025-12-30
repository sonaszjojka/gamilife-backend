package pl.gamilife.groupshop.application.purchasegroupitem;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record PurchaseGroupItemResult(
        UUID ownedGroupItemId,
        UUID groupItemId,
        UUID groupMemberId,
        Instant acquiredAt
) {
}
