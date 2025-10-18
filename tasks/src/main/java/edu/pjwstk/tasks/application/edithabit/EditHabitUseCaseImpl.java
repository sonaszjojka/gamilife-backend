package edu.pjwstk.tasks.application.edithabit;

import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.exception.HabitNotFoundException;
import edu.pjwstk.tasks.exception.InvalidHabitDataException;
import edu.pjwstk.tasks.repository.HabitRepository;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import java.time.LocalDateTime;
import java.util.UUID;

@Component
public class EditHabitUseCaseImpl implements EditHabitUseCase {

    private final HabitRepository habitRepository;
    private final EditHabitMapper editHabitMapper;

    public EditHabitUseCaseImpl(HabitRepository habitRepository, EditHabitMapper editHabitMapper) {
        this.habitRepository = habitRepository;
        this.editHabitMapper = editHabitMapper;
    }

    @Override
    @Transactional
    public EditHabitResponse execute(EditHabitRequest request, UUID habitId) {
        Habit habit = habitRepository.findById(habitId)
                .orElseThrow(() -> new HabitNotFoundException(
                        "Habit with id " + habitId + " not found!"
                ));

        habit.setCycleLength(request.cycleLength());
        habit.setCurrentStreak(request.currentStreak());
        habit.setLongestStreak(request.longestStreak()); //todo: business logic
        habit.setIsAccepted(request.isAccepted());

        if (request.acceptedDate() != null &&
                request.acceptedDate().isBefore(LocalDateTime.now())) {
            throw new InvalidHabitDataException("Accepted date cannot be earlier than creation date");
        }

        habit.setAcceptedDate(request.acceptedDate());
        habit.setDeclineMessage(request.declineMessage());

        return editHabitMapper.toResponse(habitRepository.save(habit));
    }
}
