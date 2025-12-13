package pl.gamilife.groupshop.application.editgroupiteminshop;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record EditGroupItemInShopResponse(
        UUID groupItemInShopId,
        String name,
        Integer price,
        Boolean isActive,
        Instant createdAt,
        UUID groupShopId
) {
}
