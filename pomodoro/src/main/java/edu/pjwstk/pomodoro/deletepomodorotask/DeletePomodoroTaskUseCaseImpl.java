package edu.pjwstk.pomodoro.deletepomodorotask;

import edu.pjwstk.pomodoro.domain.PomodoroTask;
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
         if (pomodoroTaskRepository.existsByTaskId(pomodoroTaskId))
         {
             System.out.println("Weszło");
             throw new InvalidPomodoroTaskData("Task with id:" + pomodoroTaskId + " does not exist");
         }
        pomodoroTaskRepository.deleteById(pomodoroTaskId);
    }

}
