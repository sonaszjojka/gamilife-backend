package pl.gamilife.grouptask.repository.jpa;

import org.springframework.data.jpa.repository.JpaRepository;
import pl.gamilife.grouptask.entity.GroupTaskMember;

import java.util.List;
import java.util.UUID;

public interface GroupTaskMemberRepositoryJpa extends JpaRepository<GroupTaskMember, UUID> {
    List<GroupTaskMember> findByGroupTaskIdIn(List<UUID> groupTasksIds);
}
