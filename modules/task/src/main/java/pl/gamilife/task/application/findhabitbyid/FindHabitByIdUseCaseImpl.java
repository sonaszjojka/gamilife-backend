package pl.gamilife.task.application.findhabitbyid;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.shared.kernel.exception.domain.ResourceOwnerPrivilegesRequiredException;
import pl.gamilife.task.domain.exception.domain.HabitNotFoundException;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.port.context.UserContext;
import pl.gamilife.task.domain.port.repository.HabitRepository;

import java.time.LocalDate;
import java.time.ZoneId;

@Service
@AllArgsConstructor
public class FindHabitByIdUseCaseImpl implements FindHabitByIdUseCase {

    private final HabitRepository habitRepository;
    private final UserContext userContext;

    @Override
    public FindHabitByIdResult execute(FindHabitByIdCommand cmd) {
        Habit habit = habitRepository.findById(cmd.habitId()).orElseThrow(
                () -> new HabitNotFoundException("Habit not found")
        );

        if (!habit.getUserId().equals(cmd.userId())) {
            throw new ResourceOwnerPrivilegesRequiredException(String.format(
                    "User %s is not an owner of habit %s", cmd.userId(), habit.getId()
            ));
        }

        ZoneId zoneId = cmd.zoneId() == null
                ? userContext.getCurrentUserTimezone(cmd.userId())
                : cmd.zoneId();
        LocalDate currentUserDate = LocalDate.now(zoneId);
        habit.syncCurrentStreak(currentUserDate);

        return new FindHabitByIdResult(
                habit.getUserId(),
                habit.checkIfCanBeWorkedOn(currentUserDate)
        );
    }
}
