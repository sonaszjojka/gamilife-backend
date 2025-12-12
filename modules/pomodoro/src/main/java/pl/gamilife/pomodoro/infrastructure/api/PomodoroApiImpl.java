package pl.gamilife.pomodoro.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.pomodoro.PomodoroApi;
import pl.gamilife.api.pomodoro.dto.PomodoroItemDto;

import java.util.List;
import java.util.UUID;

@Service
@AllArgsConstructor
public class PomodoroApiImpl implements PomodoroApi {

    public List<PomodoroItemDto> findPomodoroItemsByTaskIds(List<UUID> taskIds) {
//        return findPomodoroTaskByTaskIdUseCase.execute(taskIds);
        return null;
    }

    @Override
    public List<PomodoroItemDto> findPomodoroItemsByHabitIds(List<UUID> habitIds) {
//        return findPomodoroTaskByTaskIdUseCase.execute(habitIds);
        return null;
    }
}
