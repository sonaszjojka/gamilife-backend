package pl.gamilife.groupshop.application.creategroupiteminshop;

import lombok.Builder;

import java.util.UUID;

@Builder
public record CreateGroupItemInShopResult(
        UUID id,
        String name,
        Integer price,
        Boolean isActive,
        UUID groupShopId

) {
}
