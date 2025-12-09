package pl.gamilife.gamification.application.usecase.getstoreitems.getbyid;

import java.io.Serializable;
import java.util.UUID;

public record StoreItemDetailsDto(
    UUID id,
    String name,
    String imagePath,
    StoreItemDetailsDto.ItemSlotDto itemSlot,
    StoreItemDetailsDto.RarityDto rarity,
    Integer price,
    String description,
    Boolean isAlreadyOwned
    )implements Serializable{

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
