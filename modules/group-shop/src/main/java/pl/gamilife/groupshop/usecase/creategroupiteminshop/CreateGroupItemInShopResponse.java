package pl.gamilife.groupshop.usecase.creategroupiteminshop;

import lombok.Builder;

import java.time.Instant;
import java.util.UUID;

@Builder
public record CreateGroupItemInShopResponse(
        UUID groupItemId,
        String name,
        Integer price,
        Instant createdAt,
        Boolean isActive,
        UUID groupShopId

) {
}
