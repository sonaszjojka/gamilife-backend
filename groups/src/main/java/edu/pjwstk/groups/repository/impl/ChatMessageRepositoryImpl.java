package edu.pjwstk.groups.repository.impl;

import edu.pjwstk.groups.entity.ChatMessage;
import edu.pjwstk.groups.repository.ChatMessageRepository;
import edu.pjwstk.groups.repository.jpa.ChatMessageRepositoryJpa;
import org.springframework.stereotype.Repository;

@Repository
public class ChatMessageRepositoryImpl implements ChatMessageRepository {

    private final ChatMessageRepositoryJpa repositoryJpa;

    public ChatMessageRepositoryImpl(ChatMessageRepositoryJpa repositoryJpa) {
        this.repositoryJpa = repositoryJpa;
    }

    @Override
    public ChatMessage save(ChatMessage chatMessage) {
        return repositoryJpa.save(chatMessage);
    }
}
