package edu.pjwstk.tasks.tasks;
import edu.pjwstk.tasks.application.deletetask.DeleteTaskUseCaseImpl;
import edu.pjwstk.tasks.entity.Task;
import edu.pjwstk.tasks.exception.TaskNotFoundException;
import edu.pjwstk.tasks.repository.TaskRepository;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Optional;
import java.util.UUID;

import static org.mockito.Mockito.*;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

@ExtendWith(MockitoExtension.class)
class DeleteTaskUseCaseTest {

    @Mock
    private TaskRepository taskRepository;

    @InjectMocks
    private DeleteTaskUseCaseImpl sut;

    @Test
    void shouldDeleteTaskSuccessfully() {
        // given
        UUID taskId = UUID.randomUUID();
        Task task = new Task();

        when(taskRepository.findById(taskId)).thenReturn(Optional.of(task));

        // when
        sut.execute(taskId);

        // then
        verify(taskRepository).findById(taskId);
        verify(taskRepository).deleteById(taskId);
        verifyNoMoreInteractions(taskRepository);
    }

    @Test
    void shouldThrowExceptionWhenTaskNotFound() {
        // given
        UUID taskId = UUID.randomUUID();
        when(taskRepository.findById(taskId)).thenReturn(Optional.empty());

        // when / then
        assertThatThrownBy(() -> sut.execute(taskId))
                .isInstanceOf(TaskNotFoundException.class)
                .hasMessageContaining("Task with id " + taskId + " not found!");

        verify(taskRepository).findById(taskId);
        verify(taskRepository, never()).deleteById(any());
    }
}
