package edu.pjwstk.tasks.repository.jpa;

import edu.pjwstk.tasks.entity.TaskCategory;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TaskCategoryRepositoryJpa extends JpaRepository<TaskCategory, Integer> {

}
