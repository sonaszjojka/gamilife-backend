package pl.gamilife.gamification.application.usecase.getuserinventoryitems;

import java.io.Serializable;
import java.util.Collection;
import java.util.UUID;

public record GetUserInventoryItemsResult(
        long totalElements,
        int totalPages,
        int currentPage,
        int pageSize,
        Collection<UserInventoryItemDto> content
) implements Serializable {

    public record UserInventoryItemDto(
            UUID id,
            UUID itemId,
            UUID userId,
            Integer quantity,
            Boolean isEquipped,
            ItemDto item
    ) implements Serializable {
    }

    public record ItemDto(
            UUID id,
            String name,
            String description,
            String imagePath,
            Integer quickSellValue,
            ItemSlotDto itemSlot,
            RarityDto rarity,
            Integer price,
            UUID achievementId,
            Integer unlockLevelId
    ) implements Serializable {
    }

    public record ItemSlotDto(
            Integer id,
            String name
    ) implements Serializable {
    }

    public record RarityDto(
            Integer id,
            String name
    ) implements Serializable {
    }
}