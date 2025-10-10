package edu.pjwstk.tasks.repository.jpa;

import edu.pjwstk.tasks.entity.Task;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.UUID;

public interface TaskRepositoryJpa extends JpaRepository<Task, UUID> {
}
