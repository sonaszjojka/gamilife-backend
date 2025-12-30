package pl.gamilife.groupshop.application.getgroupshopitems;

import java.util.UUID;

public record GetGroupShopItemsResult(
        UUID id,
        String name,
        Integer price,
        Boolean isActive
) {
}
