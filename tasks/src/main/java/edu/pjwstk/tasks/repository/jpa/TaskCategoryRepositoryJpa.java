package edu.pjwstk.tasks.repository.jpa;

import edu.pjwstk.tasks.domain.Habit;
import edu.pjwstk.tasks.domain.TaskCategory;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface TaskCategoryRepositoryJpa extends JpaRepository<TaskCategory, Integer> {

}
