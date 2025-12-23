package pl.gamilife.pomodoro.infrastructure.api;

import lombok.AllArgsConstructor;
import org.springframework.stereotype.Service;
import pl.gamilife.api.pomodoro.PomodoroApi;
import pl.gamilife.api.pomodoro.dto.ActivityItemDto;
import pl.gamilife.api.pomodoro.dto.PomodoroItemDto;
import pl.gamilife.pomodoro.application.findpomodoroitembyactivityid.FindPomodoroItemByActivityIdCommand;
import pl.gamilife.pomodoro.application.findpomodoroitembyactivityid.FindPomodoroItemByActivityIdResult;
import pl.gamilife.pomodoro.application.findpomodoroitembyactivityid.FindPomodoroItemByActivityIdUseCase;

import java.util.List;

@Service
@AllArgsConstructor
public class PomodoroApiImpl implements PomodoroApi {

    private final FindPomodoroItemByActivityIdUseCase findPomodoroItemByActivityIdUseCase;

    @Override
    public List<PomodoroItemDto> findPomodoroItemsByActivityIds(List<ActivityItemDto> attachedEntities) {
        List<FindPomodoroItemByActivityIdResult> result =
                findPomodoroItemByActivityIdUseCase.execute(new FindPomodoroItemByActivityIdCommand(attachedEntities.stream()
                        .map(ai -> new FindPomodoroItemByActivityIdCommand.ActivityItem(
                                ai.id(),
                                ai.type()
                        ))
                        .toList()));

        return result.stream()
                .map(p -> new PomodoroItemDto(
                        p.activityId(),
                        p.pomodoroId(),
                        p.cyclesRequired(),
                        p.cyclesCompleted()
                ))
                .toList();
    }
}
