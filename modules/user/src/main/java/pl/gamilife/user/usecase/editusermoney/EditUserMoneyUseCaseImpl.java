package pl.gamilife.user.usecase.editusermoney;

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
public class EditUserMoneyUseCaseImpl implements EditUserMoneyUseCase {

    private final UserRepository userRepository;

    @Override
    public BasicUserInfoDto execute(EditUserMoneyCommand cmd) {
        User user = getUser(cmd.userId());

        user.editMoneyBy(cmd.money());

        userRepository.save(user);

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
