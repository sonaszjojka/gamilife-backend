package pl.gamilife.user.usecase.grantrewardstouser;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.dto.RewardedUserApiDto;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class GrantRewardsToUserUseCaseImpl implements GrantRewardsToUserUseCase {

    private final UserRepository userRepository;

    @Override
    public RewardedUserApiDto execute(GrantRewardsToUserCommand cmd) {
        User user = getUser(cmd.userId());

        user.grantExperience(cmd.experience());
        user.grantMoney(cmd.money());

        userRepository.save(user);

        return new RewardedUserApiDto(
                user.getId(),
                user.getUsername(),
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
