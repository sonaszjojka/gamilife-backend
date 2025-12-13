package pl.gamilife.task.application.getusershabits;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import pl.gamilife.shared.kernel.architecture.Page;
import pl.gamilife.task.domain.model.Habit;
import pl.gamilife.task.domain.model.filter.HabitFilter;
import pl.gamilife.task.domain.port.context.UserContext;
import pl.gamilife.task.domain.port.repository.HabitRepository;

import java.time.LocalDate;
import java.time.ZoneId;

@Service
@Transactional(readOnly = true)
@AllArgsConstructor
public class GetUsersHabitsUseCaseImpl implements GetUsersHabitsUseCase {

    private final HabitRepository habitRepository;
    private final UserContext userContext;

    @Override
    public Page<GetUsersHabitsResult> execute(GetUsersHabitsCommand cmd) {
        ZoneId zoneId = cmd.zoneId() == null
                ? userContext.getCurrentUserTimezone(cmd.userId())
                : cmd.zoneId();
        Page<Habit> page = habitRepository.findAllHabitsFiltered(
                new HabitFilter(
                        cmd.userId(),
                        cmd.title(),
                        cmd.categoryId(),
                        cmd.difficultyId(),
                        cmd.isAlive(),
                        LocalDate.now(zoneId)
                ),
                cmd.pageNumber(),
                cmd.pageSize()
        );

        return page.map(habit -> new GetUsersHabitsResult(
                habit.getId(),
                habit.getTitle(),
                habit.getDescription(),
                habit.getCurrentDeadline(),
                habit.getCategory().getId(),
                habit.getCategory().getName(),
                habit.getDifficulty().getId(),
                habit.getDifficulty().getName(),
                habit.getCycleLength(),
                habit.getCurrentStreak(),
                habit.getLongestStreak(),
                cmd.isAlive()
        ));
    }
}
