package edu.pjwstk.tasks.repository.jpa;

import edu.pjwstk.tasks.entity.Task;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.List;
import java.util.UUID;

public interface TaskRepositoryJpa extends JpaRepository<Task, UUID>, JpaSpecificationExecutor<Task> {

}
