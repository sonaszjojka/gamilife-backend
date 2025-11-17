package edu.pjwstk.gamification.usecase.getalllevelwithrewards;

import edu.pjwstk.gamification.repository.LevelRepository;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@AllArgsConstructor
public class GetAllLevelsWithRewardsUseCaseImpl implements GetAllLevelsWithRewardsUseCase {

    private final LevelRepository levelRepository;

    @Override
    public GetAllLevelsWithRewardsResult executeInternal(GetAllLevelsWithRewardsCommand command) {
        return new GetAllLevelsWithRewardsResult(
                levelRepository.findAllWithItemsByOrderByLevelAsc()
                        .stream()
                        .map(level -> new GetAllLevelsWithRewardsResult.LevelDto(
                                level.getLevel(),
                                level.getRequiredExperience(),
                                level.getItems().stream()
                                        .map(reward -> new GetAllLevelsWithRewardsResult.ItemDto(
                                                reward.getId(),
                                                reward.getName(),
                                                reward.getImagePath(),
                                                reward.getDescription(),
                                                new GetAllLevelsWithRewardsResult.RarityDto(
                                                        reward.getRarity().getId(),
                                                        reward.getRarity().getName()
                                                ),
                                                new GetAllLevelsWithRewardsResult.ItemSlotDto(
                                                        reward.getItemSlot().getId(),
                                                        reward.getItemSlot().getName()
                                                )
                                        ))
                                        .toList()
                        ))
                        .toList()
        );
    }
}
