package pl.gamilife.user.usecase.grantrewardstouser;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.api.user.dto.BasicUserInfoDto;
import pl.gamilife.shared.kernel.exception.domain.UserNotFoundException;
import pl.gamilife.user.persistence.User;
import pl.gamilife.user.persistence.UserRepository;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class GrantRewardsToUserUseCaseImpl implements GrantRewardsToUserUseCase {

    private final UserRepository userRepository;

    @Override
    public BasicUserInfoDto execute(GrantRewardsToUserCommand cmd) {
        User user = getUser(cmd.userId());

        if (cmd.experience() > 0) {
            user.grantExperience(cmd.experience());
        }

        if (cmd.money() > 0) {
            user.grantMoney(cmd.money());
        }

        user = userRepository.save(user);

        return new BasicUserInfoDto(
                user.getId(),
                user.getEmail(),
                user.getUsername(),
                user.getLevel(),
                user.getExperience(),
                user.getMoney(),
                user.getStatsVersion()
        );
    }

    private User getUser(UUID userId) {
        return userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }
}
