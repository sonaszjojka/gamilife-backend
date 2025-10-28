package edu.pjwstk.tasks.tasks;

import edu.pjwstk.tasks.application.edittask.EditTaskMapper;
import edu.pjwstk.tasks.application.edittask.EditTaskRequest;
import edu.pjwstk.tasks.application.edittask.EditTaskResponse;
import edu.pjwstk.tasks.application.edittask.EditTaskUseCaseImpl;
import edu.pjwstk.tasks.entity.Habit;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.entity.TaskCategory;
import edu.pjwstk.tasks.entity.TaskDifficulty;
import edu.pjwstk.tasks.repository.HabitRepository;
import edu.pjwstk.tasks.repository.TaskCategoryRepository;
import edu.pjwstk.tasks.repository.TaskDifficultyRepository;
import edu.pjwstk.tasks.repository.TaskRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.Optional;
import java.util.UUID;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class EditTaskUseCaseImplTest {

    @Mock
    private TaskRepository taskRepository;

    @Mock
    private EditTaskMapper editTaskMapper;

    @Mock
    private TaskDifficultyRepository taskDifficultyRepository;

    @Mock
    private TaskCategoryRepository taskCategoryRepository;

    @Mock
    private HabitRepository habitRepository;

    @InjectMocks
    private EditTaskUseCaseImpl editTaskUseCase;

    @Test
    void shouldEditTaskSuccessfully() {

        UUID taskId = UUID.randomUUID();
        UUID userId = UUID.randomUUID();

        TaskCategory oldCategory = TaskCategory.builder()
                .id(1)
                .title("Work")
                .value(10)
                .build();

        TaskDifficulty oldDifficulty = TaskDifficulty.builder()
                .id(1)
                .title("Medium")
                .value(5)
                .build();

        Task existingTask = Task.builder()
                .id(taskId)
                .title("Old Title")
                .description("Old Description")
                .startTime(LocalDateTime.now().minusDays(1))
                .endTime(LocalDateTime.now().plusDays(1))
                .userId(userId)
                .category(oldCategory)
                .difficulty(oldDifficulty)
                .build();

        TaskCategory newCategory = TaskCategory.builder()
                .id(2)
                .title("Personal")
                .value(15)
                .build();

        TaskDifficulty newDifficulty = TaskDifficulty.builder()
                .id(2)
                .title("Hard")
                .value(10)
                .build();

        UUID habitId = UUID.randomUUID();
        Habit habit = Habit.builder()
                .id(habitId)
                .cycleLength(Duration.ofDays(2))
                .currentStreak(5)
                .longestStreak(10)
                .isAccepted(true)
                .acceptedDate(LocalDateTime.now())
                .build();

        UUID previousTaskId = UUID.randomUUID();
        Task previousTask = Task.builder()
                .id(previousTaskId)
                .title("Previous Task")
                .startTime(LocalDateTime.now().minusDays(2))
                .userId(userId)
                .category(oldCategory)
                .difficulty(oldDifficulty)
                .build();

        LocalDateTime newStartTime = LocalDateTime.now();
        LocalDateTime newEndTime = LocalDateTime.now().plusDays(3);
        LocalDateTime completedAt = LocalDateTime.now().plusDays(2);

        EditTaskRequest request = new EditTaskRequest(
                "Updated Title",
                newStartTime,
                newEndTime,
                newCategory.getId(),
                newDifficulty.getId(),
                completedAt,
                habitId,
                previousTaskId,
                "Updated Description"
        );

        EditTaskResponse expectedResponse = EditTaskResponse.builder()
                .taskId(taskId)
                .title("Updated Title")
                .startTime(newStartTime)
                .endTime(newEndTime)
                .categoryId(newCategory.getId())
                .difficultyId(newDifficulty.getId())
                .userId(userId)
                .completedAt(completedAt)
                .habitTaskId(habitId)
                .previousTaskId(previousTaskId)
                .description("Updated Description")
                .build();

        when(taskRepository.findById(taskId)).thenReturn(Optional.of(existingTask));
        when(taskCategoryRepository.findById(newCategory.getId())).thenReturn(Optional.of(newCategory));
        when(taskDifficultyRepository.findById(newDifficulty.getId())).thenReturn(Optional.of(newDifficulty));
        when(habitRepository.findById(habitId)).thenReturn(Optional.of(habit));
        when(taskRepository.findById(previousTaskId)).thenReturn(Optional.of(previousTask));
        when(taskRepository.save(any(Task.class))).thenReturn(existingTask);
        when(editTaskMapper.toResponse(any(Task.class))).thenReturn(expectedResponse);


        EditTaskResponse result = editTaskUseCase.execute(request, taskId);


        assertThat(result).isNotNull();
        assertThat(result.taskId()).isEqualTo(taskId);
        assertThat(result.title()).isEqualTo("Updated Title");
        assertThat(result.description()).isEqualTo("Updated Description");
        assertThat(result.startTime()).isEqualTo(newStartTime);
        assertThat(result.endTime()).isEqualTo(newEndTime);
        assertThat(result.completedAt()).isEqualTo(completedAt);
        assertThat(result.categoryId()).isEqualTo(newCategory.getId());
        assertThat(result.difficultyId()).isEqualTo(newDifficulty.getId());
        assertThat(result.userId()).isEqualTo(userId);
        assertThat(result.habitTaskId()).isEqualTo(habitId);
        assertThat(result.previousTaskId()).isEqualTo(previousTaskId);

        assertThat(existingTask.getTitle()).isEqualTo("Updated Title");
        assertThat(existingTask.getDescription()).isEqualTo("Updated Description");
        assertThat(existingTask.getStartTime()).isEqualTo(newStartTime);
        assertThat(existingTask.getEndTime()).isEqualTo(newEndTime);
        assertThat(existingTask.getCompletedAt()).isEqualTo(completedAt);
        assertThat(existingTask.getCategory()).isEqualTo(newCategory);
        assertThat(existingTask.getDifficulty()).isEqualTo(newDifficulty);
        assertThat(existingTask.getHabitTask()).isEqualTo(habit);
        assertThat(existingTask.getPreviousTask()).isEqualTo(previousTask);

        verify(taskRepository, times(2)).findById(any(UUID.class));
        verify(taskCategoryRepository).findById(newCategory.getId());
        verify(taskDifficultyRepository).findById(newDifficulty.getId());
        verify(habitRepository).findById(habitId);
        verify(taskRepository).save(existingTask);
        verify(editTaskMapper).toResponse(existingTask);
    }
}