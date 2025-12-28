package pl.gamilife.gamification.application.usecase.getalllevelwithrewards;

import java.util.List;
import java.util.UUID;

public record GetAllLevelsWithRewardsResult(
        List<LevelDto> levels
) {
    public record LevelDto(
            Integer level,
            Integer requiredExperience,
            List<ItemDto> rewards
    ) {
    }

    public record ItemDto(
            UUID id,
            String name,
            String imagePath,
            String description,
            RarityDto rarity,
            ItemSlotDto itemSlot
    ) {
    }

    public record RarityDto(
            Integer id,
            String name
    ) {
    }

    public record ItemSlotDto(
            Integer id,
            String name
    ) {
    }
}
