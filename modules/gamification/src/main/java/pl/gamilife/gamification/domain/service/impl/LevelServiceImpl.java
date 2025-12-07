package pl.gamilife.gamification.domain.service.impl;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.gamification.domain.model.Item;
import pl.gamilife.gamification.domain.model.Level;
import pl.gamilife.gamification.domain.port.context.UserContext;
import pl.gamilife.gamification.domain.port.repository.LevelRepository;
import pl.gamilife.gamification.domain.service.LevelService;
import pl.gamilife.gamification.domain.service.UserInventoryService;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

@Service
@AllArgsConstructor
public class LevelServiceImpl implements LevelService {

    private final UserInventoryService userInventoryService;
    private final LevelRepository levelRepository;
    private final UserContext userContext;

    @Override
    public void levelUpUser(UUID userId, List<Level> gainedLevels) {
        Set<Item> rewardsForLevels = new HashSet<>();
        for (Level level : gainedLevels) {
            rewardsForLevels.addAll(level.getItems());
        }

        userInventoryService.addItemsToUsersInventory(userId, rewardsForLevels);
        userContext.levelUpUser(userId, gainedLevels.getLast().getLevel());
    }

    @Override
    public List<Level> checkIfUserEligibleForLevelUp(int currentLevel, int newExperience) {
        return levelRepository.findLevelsGained(
                currentLevel,
                newExperience
        );
    }
}
