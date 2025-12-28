package pl.gamilife.pomodoro.infrastructure.event.handler;

import lombok.AllArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import pl.gamilife.pomodoro.application.deletepomodoroitemforactivity.DeletePomodoroItemForActivityCommand;
import pl.gamilife.pomodoro.application.deletepomodoroitemforactivity.DeletePomodoroItemForActivityUseCase;
import pl.gamilife.pomodoro.application.undopomodoroitemfortask.UndoPomodoroItemForTaskCommand;
import pl.gamilife.pomodoro.application.undopomodoroitemfortask.UndoPomodoroItemForTaskUseCase;
import pl.gamilife.shared.kernel.event.ActivityDeletionRequestedEvent;
import pl.gamilife.shared.kernel.event.TaskUndoneEvent;

@Component
@AllArgsConstructor
public class PomodoroEventHandler {

    private final DeletePomodoroItemForActivityUseCase deletePomodoroItemForActivityUseCase;
    private final UndoPomodoroItemForTaskUseCase undoPomodoroItemForTaskUseCase;

    @EventListener
    public void onActivityDeletionRequested(ActivityDeletionRequestedEvent event) {
        deletePomodoroItemForActivityUseCase.execute(new DeletePomodoroItemForActivityCommand(
                event.activityType(),
                event.activityId()
        ));
    }

    @EventListener
    public void onTaskUndone(TaskUndoneEvent event) {
        undoPomodoroItemForTaskUseCase.execute(new UndoPomodoroItemForTaskCommand(
                event.userId(),
                event.taskId()
        ));
    }

}
