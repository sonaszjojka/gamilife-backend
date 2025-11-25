package edu.pjwstk.gamification.service.impl;

import edu.pjwstk.api.user.UserApi;
import edu.pjwstk.api.user.dto.RewardedUserApiDto;
import edu.pjwstk.gamification.model.Level;
import edu.pjwstk.gamification.repository.LevelRepository;
import edu.pjwstk.gamification.service.RewardService;
import edu.pjwstk.gamification.service.UserInventoryService;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.UUID;

@Service
@AllArgsConstructor
@Slf4j
public class RewardServiceImpl implements RewardService {

    private final UserApi userApi;
    private final LevelRepository levelRepository;
    private final UserInventoryService userInventoryService;

    @Override
    @Transactional
    public void rewardUser(UUID userId, int experience, int money) {
        RewardedUserApiDto rewardedUser = userApi.grantRewardsToUser(userId, experience, money);

        List<Level> level = levelRepository.findWithRewardsForExperienceAmount(
                rewardedUser.experience(),
                PageRequest.of(0, 1)
        );

        if (level.isEmpty()) {
            log.warn("Could not get level for experience amount: {}", rewardedUser.experience());
            return;
        }

        checkAndLevelUpUser(rewardedUser, level.getFirst());
    }

    private void checkAndLevelUpUser(RewardedUserApiDto user, Level level) {
        if (user.level() < level.getLevel()) {
            userApi.levelUpUser(user.userId(), level.getLevel());
            userInventoryService.addItemsToUsersInventory(user.userId(), level.getItems());
        }
    }
}
