package pl.gamilife.groupshop.application.editgroupitem;

import lombok.Builder;

import java.util.UUID;

@Builder
public record EditGroupItemResult(
        UUID groupItemInShopId,
        String name,
        Integer price,
        Boolean isActive,
        UUID groupShopId
) {
}
