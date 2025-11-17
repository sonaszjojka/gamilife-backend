package edu.pjwstk.gamification.model;

import edu.pjwstk.gamification.enums.ItemSlotEnum;
import edu.pjwstk.gamification.enums.RarityEnum;
import jakarta.persistence.*;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import lombok.Getter;
import lombok.ToString;
import org.hibernate.annotations.Immutable;

import java.util.UUID;

@Getter
@Entity
@ToString(exclude = {"itemSlot", "rarity", "achievement", "unlockLevel"})
@Table(name = "item")
@Immutable
public class Item {
    @Id
    @Column(name = "id", nullable = false)
    private UUID id;

    @Size(max = 150)
    @NotNull
    @Column(name = "name", nullable = false, length = 150)
    private String name;

    @Size(max = 255)
    @NotNull
    @Column(name = "description", nullable = false)
    private String description;

    @Size(max = 255)
    @NotNull
    @Column(name = "image_path", nullable = false)
    private String imagePath;

    @NotNull
    @Column(name = "quick_sell_value", nullable = false)
    private Integer quickSellValue;

    @Column(name = "item_slot_id", nullable = false)
    private Integer itemSlotId;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "item_slot_id", nullable = false, insertable = false, updatable = false)
    private ItemSlot itemSlot;

    @Column(name = "rarity_id", nullable = false)
    private Integer rarityId;

    @NotNull
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "rarity_id", nullable = false, insertable = false, updatable = false)
    private Rarity rarity;

    @Column(name = "price")
    private Integer price;

    @Column(name = "achievement_id", insertable = false, updatable = false)
    private UUID achievementId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "achievement_id")
    private Achievement achievement;

    @Column(name = "unlock_level", insertable = false, updatable = false)
    private Integer unlockLevelId;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "unlock_level")
    private Level unlockLevel;

    public ItemSlotEnum getItemSlotEnum() {
        return ItemSlotEnum.fromId(this.itemSlotId);
    }

    public RarityEnum getRarityEnum() {
        return RarityEnum.fromId(this.rarityId);
    }

}