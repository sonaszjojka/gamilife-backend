package edu.pjwstk.user.usecase.grantrewardstouser;

import edu.pjwstk.api.user.dto.RewardedUserApiDto;
import edu.pjwstk.core.exception.common.domain.UserNotFoundException;
import edu.pjwstk.user.domain.User;
import edu.pjwstk.user.persistence.UserRepository;
import jakarta.transaction.Transactional;
import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@AllArgsConstructor
public class GrantRewardsToUserUseCaseImpl implements GrantRewardsToUserUseCase {

    private final UserRepository userRepository;

    @Override
    @Transactional
    public RewardedUserApiDto executeInternal(GrantRewardsToUserCommand cmd) {
        User user = getUser(cmd.userId());

        user.setExperience(user.getExperience() + cmd.experience());
        user.setMoney(user.getMoney() + cmd.money());

        userRepository.save(user);

        return new RewardedUserApiDto(
                user.getId(),
                user.getExperience(),
                user.getMoney(),
                user.getLevel()
        );
    }

    private User getUser(UUID userId) {
        return userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }
}
