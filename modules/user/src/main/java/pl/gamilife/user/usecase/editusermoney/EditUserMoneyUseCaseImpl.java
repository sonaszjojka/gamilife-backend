package pl.gamilife.user.usecase.editusermoney;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.infrastructure.core.exception.domain.UserHasNotEnoughMoneyException;
import pl.gamilife.infrastructure.core.exception.domain.UserNotFoundException;
import pl.gamilife.user.domain.User;
import pl.gamilife.user.persistence.UserRepository;

import java.util.UUID;

@Service
@Transactional
@AllArgsConstructor
public class EditUserMoneyUseCaseImpl implements EditUserMoneyUseCase {

    private final UserRepository userRepository;

    @Override
    public Integer execute(EditUserMoneyCommand cmd) {
        User user = getUser(cmd.userId());

        int newUserMoney = user.getMoney() + cmd.money();
        if (newUserMoney < 0) {
            throw new UserHasNotEnoughMoneyException("User has not enough money");
        }

        userRepository.updateUserMoney(user.getId(), newUserMoney);

        return newUserMoney;
    }

    private User getUser(UUID userId) {
        return userRepository.getUserById(userId)
                .orElseThrow(() -> new UserNotFoundException("User not found"));
    }
}
