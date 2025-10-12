package edu.pjwstk.tasks.application.createhabit;

import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.exception.InvalidHabitDataException;
import edu.pjwstk.tasks.repository.HabitRepository;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.util.UUID;

@Component
public class CreateHabitUseCaseImpl implements CreateHabitUseCase {

    private final HabitRepository habitRepository;
    private final CreateHabitMapper habitMapper;

    public CreateHabitUseCaseImpl(HabitRepository habitRepository, CreateHabitMapper habitMapper) {
        this.habitRepository = habitRepository;
        this.habitMapper = habitMapper;
    }

    @Override
    public CreateHabitResponse execute(CreateHabitRequest request) {
        if (request.acceptedDate() != null &&
                request.acceptedDate().isBefore(LocalDateTime.now())) {
            throw new InvalidHabitDataException("Accepted date cannot be earlier than creation date");
        }

        Habit habit = habitRepository.save(habitMapper.toEntity(request, UUID.randomUUID()));
        return habitMapper.toResponse(habit);
    }
}
