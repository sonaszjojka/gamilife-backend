package edu.pjwstk.tasks.tasks;

import edu.pjwstk.tasks.application.createtask.CreateTaskMapper;
import edu.pjwstk.tasks.application.createtask.CreateTaskRequest;
import edu.pjwstk.tasks.application.createtask.CreateTaskResponse;
import edu.pjwstk.tasks.application.createtask.CreateTaskUseCaseImpl;
import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskCategory;
import edu.pjwstk.tasks.entity.TaskDifficulty;
import edu.pjwstk.tasks.repository.HabitRepository;
import edu.pjwstk.tasks.repository.TaskCategoryRepository;
import edu.pjwstk.tasks.repository.TaskDifficultyRepository;
import edu.pjwstk.tasks.repository.impl.TaskRepositoryImpl;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class CreateTaskUseCaseTest {

    @Mock private TaskRepositoryImpl taskRepository;
    @Mock private TaskCategoryRepository categoryRepository;
    @Mock private TaskDifficultyRepository difficultyRepository;
    @Mock private HabitRepository habitRepository;
    @Mock private CreateTaskMapper mapper;

    @InjectMocks
    private CreateTaskUseCaseImpl sut;

    @Test
    void shouldCreateTaskSuccessfully() {


        Integer categoryId = 1;
        Integer difficultyId = 1;
        UUID habitId = UUID.randomUUID();
        UUID previousTaskId = UUID.randomUUID();
        UUID userId = UUID.randomUUID();

        CreateTaskRequest request = new CreateTaskRequest(
                "Task",
                LocalDateTime.now(),
                LocalDateTime.now().plusHours(2),
                categoryId,
                difficultyId,
                userId,
                null,
                habitId,
                previousTaskId,
                "Description"
        );

        TaskCategory category = new TaskCategory();
        TaskDifficulty difficulty = new TaskDifficulty();
        Habit habit = new Habit();
        Task previousTask = new Task();
        Task savedTask = new Task();
        CreateTaskResponse expectedResponse = new CreateTaskResponse(UUID.randomUUID(),request.title(), request.startTime(), request.endTime(),
                request.categoryId(), request.difficultyId(), request.userId(), request.completedAt(),
                request.habitTaskId(), request.previousTaskId(), request.description());

        when(categoryRepository.findById(categoryId)).thenReturn(Optional.of(category));
        when(difficultyRepository.findById(difficultyId)).thenReturn(Optional.of(difficulty));
        when(habitRepository.findById(habitId)).thenReturn(Optional.of(habit));
        when(taskRepository.findById(previousTaskId)).thenReturn(Optional.of(previousTask));
        when(mapper.toEntity(eq(request), any(UUID.class), eq(category), eq(difficulty), eq(habit), eq(previousTask)))
                .thenReturn(savedTask);
        when(taskRepository.save(savedTask)).thenReturn(savedTask);
        when(mapper.toResponse(savedTask)).thenReturn(expectedResponse);


        CreateTaskResponse response = sut.execute(request);


        assertThat(response).isEqualTo(expectedResponse);

        verify(taskRepository).save(savedTask);
        verify(mapper).toResponse(savedTask);
    }
}
