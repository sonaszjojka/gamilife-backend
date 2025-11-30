package edu.pjwstk.grouptasks.repository.jpa;

import edu.pjwstk.grouptasks.entity.GroupTask;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;

import java.util.UUID;

public interface GroupTaskRepositoryJpa extends JpaRepository<GroupTask, UUID> , JpaSpecificationExecutor<GroupTask> {
}
