package pl.gamilife.pomodoro.infrastructure.event.handler;

import lombok.AllArgsConstructor;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;
import pl.gamilife.pomodoro.application.deletepomodoroitemforactivity.DeletePomodoroItemForActivityCommand;
import pl.gamilife.pomodoro.application.deletepomodoroitemforactivity.DeletePomodoroItemForActivityUseCase;
import pl.gamilife.shared.kernel.event.ActivityDeletionRequestedEvent;

@Component
@AllArgsConstructor
public class ActivityDeletionRequestedEventHandler {

    private final DeletePomodoroItemForActivityUseCase deletePomodoroItemForActivityUseCase;

    @EventListener
    public void onActivityDeletionRequested(ActivityDeletionRequestedEvent event) {
        deletePomodoroItemForActivityUseCase.execute(new DeletePomodoroItemForActivityCommand(
                event.activityType(),
                event.activityId()
        ));
    }

}
