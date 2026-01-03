package pl.gamilife.groupshop.application.getownedgroupitems;

import java.io.Serializable;
import java.time.Instant;
import java.util.UUID;

public record GetOwnedGroupItemsResult(
        UUID id,
        UUID memberId,
        Instant usedAt,
        GroupShopItemDto groupItem
) implements Serializable {

    public record GroupShopItemDto(
            UUID id,
            String name,
            Integer price,
            Boolean isActive
    ) {
    }

}
