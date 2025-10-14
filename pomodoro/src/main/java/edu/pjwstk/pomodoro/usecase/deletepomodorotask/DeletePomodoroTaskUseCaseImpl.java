package edu.pjwstk.pomodoro.usecase.deletepomodorotask;

import edu.pjwstk.pomodoro.exception.InvalidPomodoroTaskData;
import edu.pjwstk.pomodoro.repository.PomodoroTaskRepository;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import java.util.UUID;

@Service
public class DeletePomodoroTaskUseCaseImpl implements DeletePomodoroTaskUseCase
{
    private final PomodoroTaskRepository pomodoroTaskRepository;
    public DeletePomodoroTaskUseCaseImpl(PomodoroTaskRepository pomodoroTaskRepository) {
        this.pomodoroTaskRepository = pomodoroTaskRepository;
    }

    @Override
    @Transactional
    public void execute(UUID pomodoroTaskId) {
         if (!pomodoroTaskRepository.existsByPomodoroTaskId(pomodoroTaskId))
         {
             throw new InvalidPomodoroTaskData("Pomodoro Task with id:" + pomodoroTaskId + " does not exist");
         }
        pomodoroTaskRepository.deleteByPomodoroTaskId(pomodoroTaskId);
    }

}
