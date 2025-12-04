package pl.gamilife.grouptask.repository.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import pl.gamilife.grouptask.entity.GroupTask;

import java.util.UUID;

public interface GroupTaskRepositoryJpa extends JpaRepository<GroupTask, UUID>, JpaSpecificationExecutor<GroupTask> {
}
