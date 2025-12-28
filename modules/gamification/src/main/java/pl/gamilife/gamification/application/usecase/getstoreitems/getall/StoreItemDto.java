package pl.gamilife.gamification.application.usecase.getstoreitems.getall;

import java.io.Serializable;
import java.util.UUID;

public record StoreItemDto(
        UUID id,
        String name,
        String imagePath,
        ItemSlotDto itemSlot,
        RarityDto rarity,
        Integer price
) implements Serializable {

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





